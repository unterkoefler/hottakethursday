class AddMoreInfoToUsers < ActiveRecord::Migration[6.0]
  def change
    add_column :users, :full_name, :string, null: false, default: ""
    add_column :users, :bio, :string, null: false, default: ""
    add_column :users, :least_fav_color, :string, null: false, default: ""
  end
end
