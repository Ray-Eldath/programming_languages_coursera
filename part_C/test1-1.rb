class Point
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    puts self.class.name # self bounds to the receiver of to_s because of dynamic dispatch
    "(#{x}, #{y})"
  end
end

class ColorPoint < Point
  attr_accessor :color

  def initialize(x, y, color = :unset)
    super(x, y)
    @color = color
  end

  def to_s
    "(#{x}, #{y}) #{color}"
  end

  def dist
    if color == :unset
      0
    else
      super
    end
  end
end

class FakePoint < Point
  def initialize
  end

  def x
    42
  end

  def y
    32
  end

  # call FakePoint.new.to_s here involve dynamic dispatch
end
