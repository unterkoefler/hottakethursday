class TakeSerializer < ActiveModel::Serializer
  attributes :id, :contents, :created_at, :number_of_upvotes
  belongs_to :user
end
