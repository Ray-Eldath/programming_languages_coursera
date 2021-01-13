# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  All_My_Pieces = All_Pieces + [
    [[[0, 0], [1, 0], [2, 0], [-1, 0], [-2, 0]],
     [[0, 0], [0, 1], [0, 2], [0, -1], [0, -2]]], # long-long
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]),
    rotations([[0, 0], [0, 1], [1, 0]]) # short L
  ]

  Cheat_Piece = [[[0, 0]]]

  def self.next_piece(board, cheating = false)
    MyPiece.new((cheating ? Cheat_Piece : All_My_Pieces).sample, board)
  end
end

class MyBoard < Board
  # your enhancements here

  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheating = false
  end

  def next_piece
    super
    @current_block = MyPiece.next_piece(self, @cheating)
    @cheating = false
  end

  def cheat
    return if score < 100 || @cheating

    @score -= 100
    @cheating = true
    draw
  end

  def flip
    @current_block.move(0, 0, 2) if !game_over? && @game.is_running?
    draw
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each do |index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    end
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { @board.flip })
    @root.bind('c', proc { @board.cheat })
  end
end
