class RenameVoteToLike < ActiveRecord::Migration[6.0]
  def change
    rename_table :votes, :likes
  end
end
