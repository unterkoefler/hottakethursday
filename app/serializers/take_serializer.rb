class TakeSerializer < ActiveModel::Serializer
  attributes :id, :contents, :created_at, :number_of_likes
  belongs_to :user
  has_many :users_who_liked
end
