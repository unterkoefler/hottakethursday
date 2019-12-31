class TakeSerializer < ActiveModel::Serializer
  attributes :id, :contents, :created_at, :number_of_likes
  belongs_to :user

  attribute :users_who_liked
  def users_who_liked
    object.users_who_liked.map do |u|
      UserSerializer.new(u)
    end
  end
end
