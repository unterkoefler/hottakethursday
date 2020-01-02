class UserSerializer < ActiveModel::Serializer
  attributes :id, :username, :avatar_url
end
