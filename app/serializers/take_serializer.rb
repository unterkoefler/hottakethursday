class TakeSerializer < ActiveModel::Serializer
  attributes :id, :contents, :created_at, :number_of_likes, :users_who_liked
  belongs_to :user
end
