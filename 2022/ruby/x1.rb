def parse
  file = File.open("../inputs/1", "r")
  contents = file.read
                 .split("\n\n")
                 .map { |x| x.lines }
                 .map { |x| x.map {|y| Integer(y) } }
                 .map { |x| x.sum }

  file.close
  return contents
end

if __FILE__ == $0
  puts parse.max
  puts parse.sort.reverse.take(3).sum
end
