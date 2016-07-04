require 'rubygems' if RUBY_VERSION < '1.9'
begin
  require 'awesome_print'
  rescue LoadError
end

Pry.print = proc{|output,value| output.puts value.ai }
