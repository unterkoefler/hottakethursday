class CreateTakes < ActiveRecord::Migration[6.0]
  def change
    create_table :takes do |t|
      t.string :contents
      t.belongs_to :user, null: false, foreign_key: true

      t.timestamps
    end
  end
end
