# By Tom Crayford
# http://www.tcrayford.net/2009/12/08/Lein-Autotest.html

watch( 'test/.*\.clj' )  {|md| test_stuff }
watch( 'src/.*\.clj' )  {|md| test_stuff }

def colorize(text, color_code)
  "#{color_code}#{text}\e[1;37m"
end

def red(text); colorize(text, "\e[0;31m"); end
def green(text); colorize(text, "\e[0;32m"); end

def match_testing(output)
  for line in output.split("\n")
    line += "\n"
    if line.match(/cake test|----|Finished|Testing.*|0 failures.*|Ran \d+ tests containing \d+ assertions.*/)
      print green(line)
    else
      print red(line)
    end
  end
end

def test_stuff
  match_testing(`cake test`)
  puts "#{Time.now.strftime("%I:%M %p")} ======================="
end
