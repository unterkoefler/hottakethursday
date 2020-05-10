class User < ApplicationRecord
  has_many :takes, dependent: :destroy
  has_many :likes, dependent: :destroy

  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable, :trackable and :omniauthable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :validatable,
         :confirmable,
         :jwt_authenticatable,
         jwt_revocation_strategy: JwtBlacklist

  # https://thinkster.io/tutorials/rails-json-api/setting-up-users-and-authentication-for-our-api
  validates :username,
            uniqueness: { case_sensitive: false },
            presence: true,
            allow_blank: false,
            format: { with: /\A[a-zA-Z0-9]+\z/ }
  validates :birthday, presence: true
  validate :must_be_old_enough

  # https://medium.com/@mazik.wyry/rails-5-api-jwt-setup-in-minutes-using-devise-71670fd4ed03

  def must_be_old_enough
    if birthday + 16.years > Date.today
      errors.add :birthday, "Aren't you a bit young for this?"
    end
  end

  def fill_default_username!
    adjectives = %w[Pernicious Volatile Cuddly Ferocious Malignant Spicy Taken Ecstatic Sweet]
    nouns = %w[Penguin Dolphin PolarBear Tiger Platypus Salmon Wolverine Cat Dog Elephant]
    self.username ||= adjectives.sample + nouns.sample + rand(1000).to_s
  end

  after_initialize :fill_default_username!

  has_one_attached :avatar

  def avatar_url
    return nil unless avatar.attached?

    Rails.application.routes.url_helpers.rails_blob_path(avatar, only_path: true)
  end

  def make_the_hottest_of_takes!(words)
    Take.create!(contents: words, user: self)
  end

  def like!(take)
    unless take.likes.where(user_id: id).any?
      Like.create!(user: self, take: take)
    end
  end

  def unlike!(take)
    take.likes.where(user_id: id).destroy_all
  end
end
