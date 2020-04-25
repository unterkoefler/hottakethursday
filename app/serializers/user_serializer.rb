class UserSerializer < ActiveModel::Serializer
  attributes :id, :username, :avatar_url, :full_name, :bio, :least_fav_color
end
