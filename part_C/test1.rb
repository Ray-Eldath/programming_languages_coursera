class MyRational
  def initialize(x, y = 1)
    raise "dominator is invalid" if y == 0
    if y < 0
      @x = -x
      @y = -y
    else
      @x = x
      @y = y
    end
    reduce!
  end

  def +(r)
    MyRational.new(@x * r.y + r.x * @y,
                   @y * r.y)
  end

  def to_s
    r = @x.to_s
    r += "/#{@y}" if y != 1
    r
  end

  protected def reduce!
    if @x == 0
      @y = 1
      return nil
    end
    d = gcd(@x.abs, @y)
    @x /= d
    @y /= d
  end
  attr_reader :x, :y

  private def gcd(x, y)
    if x == y
      x
    elsif x < y
      gcd(x, y - x)
    else
      gcd(y, x)
    end
  end
end
