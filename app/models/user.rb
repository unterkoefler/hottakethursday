class User < ApplicationRecord
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable, :trackable and :omniauthable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :validatable,
         :jwt_authenticatable,
         jwt_revocation_strategy: JwtBlacklist

  # https://thinkster.io/tutorials/rails-json-api/setting-up-users-and-authentication-for-our-api
  validates :username,
            uniqueness: { case_sensitive: false },
            presence: true,
            allow_blank: false,
            format: { with: /\A[a-zA-Z0-9]+\z/ }

  # https://medium.com/@mazik.wyry/rails-5-api-jwt-setup-in-minutes-using-devise-71670fd4ed03


  def fill_default_username!
    adjectives = %w[Pernicious Volatile Cuddly Ferocious Malignant Spicy Taken Ecstatic Sweet]
    nouns = %w[Penguin Dolphin PolarBear Tiger Platypus Salmon Wolverine Cat Dog Elephant]
    self.username ||= adjectives.sample + nouns.sample + rand(1000).to_s
  end

  after_initialize :fill_default_username!

  def generate_jwt
    JWT.encode({ id: id,
                 exp: 60.days.from_now.to_i },
               Rails.application.secrets.secret_key_base)
  end
end
