class TakeSerializer < ActiveModel::Serializer
  attributes :id, :contents, :created_at
  belongs_to :user
end
