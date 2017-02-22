# -*- coding: utf-8 -*-
#!/usr/bin/ruby
require 'pp'

if ARGV.size == 0
  puts "set bnf file"
end

class NonTerm
  attr_accessor :opt, :rep, :comment, :candidates
  def initialize(can, opt: false, rep: false, comment: false)
    # @token: multiple candidates array
    # @opt: optional flag [0..1]
    # @rep: repetition flag [1..n]
    @opt = opt
    @rep = rep
    @comment = comment
    if can.class == String
      if can.length == 1
        @candidates = [["'" + can + "'"]]
      else
        @candidates = [[can]]
      end
    elsif can.class == NonTerm
      @candidates = [[can]]
    elsif can.class == Array
      @candidates = can
    else
      raise "unknown type #{can.class}"
    end
  end

  def push(new_token)
    @candidates << new_token
  end

  def simple?
    return @opt == false && @comment == false && @rep == false && @candidates.size == 1
  end

  def simplify
    unless simple?
      self
    else
      if @candidates[0].class == NonTerm
        @candidates[0].simplify
      else
        @candidates[0]
      end
    end
  end

  def visit(&blk)
    @candidates.each{|cand|
      cand.each{|token|
        if token.class == String
          blk.call(token)
        elsif token.class == NonTerm
          token.visit(&blk)
        else
          raise "unknown node type"
        end
      }
    }
  end

  def add_token(seq)
    seq = seq.strip.gsub(/\s+/, " ")
    return if /^\s*$/ =~ seq
    if seq == "..."
      self[-1][:rep] = true
    elsif seq[-3..-1] == "..."
      push NonTerm.new(seq[0..-4], rep: true)
    else
      push NonTerm.new(seq)
    end
  end

  def generate_good_name
    ret = ""
    ret += "list_" if @rep
    ret += "opt_" if @opt
    if @comment
      return "<#{@candidates.join(" ")}>"
    end
    @candidates.each_with_index{|candidate, j|
      ret += "_or_" if 0 < j
      candidate.each_with_index{|token, i|
        ret += "_" if 0 < i
        if token.class == String and /[a-zA-Z0-9]/ =~ token
          ret += token.downcase
        elsif token.class == NonTerm
          may_good_name = token.generate_good_name
          return nil if may_good_name.nil?
          ret += may_good_name
        else
          return nil
        end
      }
    }
    ret
  end

  def to_s
    ret = ""
    if @comment
      ret += "<@"
    elsif @opt && @rep
      ret += "{"
    elsif @opt
      ret += "["
    elsif @rep
      ret += "<"
    else
      ret += "("
    end
    @candidates.each_with_index{|candidate, i|
      ret += " | " if 0 < i
      candidate.each_with_index{|t, j|
        ret += " " if 0 < j
        case t
        when String
          ret += t
        when NonTerm
          ret += t.to_s
        else
          raise "invalid token class #{t.class}"
        end
      }
    }
    if @comment
      ret += "@>"
    elsif @opt && @rep
      ret += "}*"
    elsif @opt
      ret += "]?"
    elsif @rep
      ret += ">+"
    else
      ret += ")"
    end
    ret
  end

  @@rule_map = {}
  @@rule_cnt = 0

  def dump_yacc(name)
    inside_rules = {}

    def dump_tokens(tokens, inside_rules)
      tokens.each_with_index{|token, i|
        print " " if 0 < i
        if token.class == String
          print token
        elsif token.class == NonTerm
          name = token.to_s
          if @@rule_map.include? name
            print @@rule_map[name]
          else
            may_good_name = token.generate_good_name
            if may_good_name.nil?
              if token.rep
                new_token = "list_" + @@rule_cnt.to_s
              elsif token.opt
                new_token = "opt_" + @@rule_cnt.to_s
              else
                new_token = "seq_" + @@rule_cnt.to_s
              end
              @@rule_cnt += 1
            else
              new_token = may_good_name
            end
            @@rule_map[name] = new_token
            inside_rules[new_token] = token
            print new_token
          end
        else
          raise "invalid term"
        end
      }
    end

    puts "#{name}:"

    if @rep
      if 1 < @candidates.size
        raise "cannot repeat multiple candidates"
      end
      print "         "
      dump_tokens(@candidates[0], inside_rules)
      puts ""
      print    "    |     "
      print "#{name} "
      dump_tokens(@candidates[0], inside_rules)
      puts ""
    else
      if @opt
        puts    "         /* Nothing */"
      end
      @candidates.each_with_index{|candidate, i|
        if i == 0 and not @opt
          print "         "
        else
          print "    |    "
        end
        if @rep
          print name + " "
        end
        dump_tokens(candidate, inside_rules)
        puts ""
      }
      puts    "         ;"
      puts    ""

      inside_rules.each{|name, struct|
        struct.dump_yacc(name)
      }
      nil
    end
  end

  def replace(target, term)
    raise "term must be array" unless term.class == Array
    recursion = term.map{|t| t.include? target}.include? true
    if recursion
      #puts "stop replaceing because #{term} include #{target}"
      return
    end
    loop do
      retry_flag = false
      @candidates.each_with_index{|candidate, j|
        candidate.size.times{|i|
          if candidate[i].class == String && candidate[i] == target
            prev = candidate[0...i]
            succ = candidate[i+1..-1]
            @candidates[j] = prev + term + succ
            retry_flag = true
            break
          elsif candidate[i].class == NonTerm
            candidate[i].replace(target, term)  # call it recursively
          end
        }
        break if retry_flag
      }
      break unless retry_flag
    end
  end

  def include? target
    found = false
    @candidates.each{|token|
      raise "tokens is not array of array #{token.to_s}" if token.class != Array
      token.each{|t|
        if t.class == String
          found = true if t == target
        elsif t.class == NonTerm
          found |= t.include? target
          break if found
        else
          raise "invalid class #{t.class}"
        end
      }
      break if found
    }
    found
  end
end

class String
  # CAUTION: monkey patch
  def tag2id
    self.strip.gsub(" ", "_").gsub(/^</, "").gsub(/>$/, "")
  end
  def as_token
    if self.is_simple_tag
      {
        status: tag,
        token: self.strip.tag2id,
        repeat: false,
        opt: false
      }
    else
      self
    end
  end
  def non_term_tokenize
    gsub(/<[^>]+>/) {|m|
      m.gsub(' ', '_')
    }.gsub(/^</, "")
      .gsub(/>$/, "")
  end
end

def file2rulemap(filename)
  prev_term = ""
  state = :neutral
  rules = []
  File.open(filename).each_line{|line|
    line.strip!
    line.scrub!('?')
    next if /^\s*$/ =~ line or /^--.*/ =~ line
    if /<.*>\s*::=/ =~ line
      non_terminal, rules_text = line.split("::=").map{|d| d.strip }
      tag_id = non_terminal.tag2id
      if rules_text.nil? or rules_text =~ /^\s*$/ or /\|$/ =~ rules_text
        # this line has no rule, goto next line
        rules << [tag_id, ""]
        prev_term = tag_id
        state = :in_rule
      else
        rules << [tag_id, rules_text.strip]
        prev_term = tag_id
        state = :neutral
      end
    elsif state == :in_rule or /\|/ =~ line
      rules[-1][1] += line.gsub(/\s+/, " ")
    else
      # puts "/* #{line} */"
    end
  }
  rules
end

def scan_matched_paren(arr, left, right)
  depth = 1
  scaned = ""
  until arr.empty?
    c = arr.shift
    case c
    when left then
      depth += 1
    when right then
      depth -= 1
    end
    if depth == 0
      break
    end
    scaned += c
  end
  if arr.empty? and 0 < depth
    raise RuntimeError
  else
    scaned
  end
end

def append_token(array, string)
  unless /^\s*$/ =~ string
    if (not array[-1].nil?) and array[-1].comment
      array[-1].candidates[0][0] += " " + string
    elsif string == "...omitted..."
      array << NonTerm.new("#omitted#")
    elsif string[0..2] == "!!"
      array << NonTerm.new("comment #{string}", comment: true)
    elsif string == "..."
      array[-1].rep = true
    elsif string[-3..-1] == "..."
      array << NonTerm.new(string[0..-4].dup.tag2id, rep: true)
    else
      array << NonTerm.new(string.dup.tag2id)
    end
  end
  string.clear
end

def analyze_structure(rulemap)
  ret = []  # | rule is pushed
  scaned = ""
  words = rulemap.split("")
  rule = []  # sequence of the rule

  state = :neutral
  until words.empty?
    c = words.shift
    case c
    when " " then
      if state == :in_bracket
        scaned += c
      elsif not /^\s*$/ =~ scaned
        append_token(rule, scaned)
      end
    when "<" then
      state = :in_bracket
      scaned += c
    when ">" then
      state = :neutral
      scaned += c
      append_token(rule, scaned)
    when "{" then
      append_token(rule, scaned)
      begin
        seq = scan_matched_paren(words, "{", "}")
        rule << NonTerm.new(analyze_structure(seq))
      rescue RuntimeError
        rule << NonTerm.new("{")
      end
    when "[" then
      append_token(rule, scaned)
      begin
        opt = scan_matched_paren(words, "[", "]")
        rule << NonTerm.new(analyze_structure(opt), opt:true)
      rescue RuntimeError
        rule << NonTerm.new("[")
      end
    when "|" then
      append_token(rule, scaned)
      ret << rule
      rule = []
    else
      scaned += c
    end
  end
  append_token(rule, scaned)
  ret << rule unless rule.empty?
  if 0 < rule.size && rule[0].comment
    rule[0]
  else
    NonTerm.new ret
  end
end

#puts analyze_structure("a {[kds]}... b...|b").to_s

class Optimizer
  def initialize
    @optimizations = []
  end
  def add_optimization(opt)
    @optimizations << opt
  end
  def optimize(rulemap)
    loop do
      changed = false
      @optimizations.each{|opt|
        changed ||= opt.call(rulemap)
      }
      break unless changed
    end
  end
end

def inline_non_opt_or_rep(rules)
  def inline(rule)
    done = false
    rule.candidates.each_with_index{|candidate, i|
      candidate.each_with_index{|token, j|
        if token.class == NonTerm
          if token.simple?
            prev = candidate[0...j]
            succ = candidate[j+1..-1]
            rule.candidates[i] = prev + token.simplify + succ
            done = true
          else
            inline(token)
          end
        end
      }
    }
    done
  end

  worked = false
  rules.each{|term, rule|
    worked |= inline(rule)
  }
  worked
end

def inline_simple_term(rules)
  worked = false
  loop do
    optimized = false
    rules.each_with_index{|r, i|
      term, rule = r
      if rule.simple?
        recursion = false
        rules.each{|t, victim|
          if victim.include? term
            victim.replace(term, rule.simplify)
            worked = true
          end
        }
        if recursion == false
          rules.delete_at i
        end
        optimized = true
        break
      end
    }
    break unless optimized
  end
  worked
end

def inline_zeromore(rules)
  worked = false
  loop do
    optimized = false
    rules.each{|term, rule|
      rule.candidates.each{|candidate|
        candidate.each{|token|
          next if token.class == String
          if token.opt && token.candidates.size == 1 &&
              token.candidates[0].size == 1 &&
              token.candidates[0][0].class == NonTerm
              token.candidates[0][0].rep
            token.rep = true
            token.candidates = token.candidates[0][0].candidates
            worked = true
            optimized = true
            break
          end
        }
      }
    }
    break unless optimized
  end
  worked
end

opt = Optimizer.new
opt.add_optimization(Proc.new{|f| inline_non_opt_or_rep(f) })
opt.add_optimization(Proc.new{|f| inline_simple_term(f) })
# opt.add_optimization(Proc.new{|f| inline_zeromore(f) }) # it may be wrong

rulemap = file2rulemap(ARGV[0])
rules = rulemap.map{|term, rules|
  if term == "space"
    ["space", NonTerm.new(" ")]
  elsif term == "vertical_bar"
    ["vertical_bar", NonTerm.new("|")]
  else
    [term, analyze_structure(rules)]
  end
}

opt.optimize(rules)

terms = []
tokens = []

rules.each{|term, rule|
  terms << term
  rule.visit{|token|
    tokens << token
  }
}
tokens = tokens.sort.uniq
terminals = tokens - terms.sort

terminals.each{|terminal|
  puts "%token #{terminal}"
}

puts ""

rules.each{|term, struct|
  struct.dump_yacc(term)
}

#output_yacc(rules)
#puts $queue
