Rails.application.routes.draw do
  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
  scope :api do
    scope :v1, defaults: { format: :json } do
      devise_for :users,
                 controllers: { sessions: 'sessions',
                                registrations: 'registrations' }
      scope :users do
        get 'me', to: 'me#me'
        get 'by_id', to: 'me#by_id'
        get 'by_ids', to: 'me#by_ids'
        scope 'me' do
          post 'change_avatar', to: 'me#change_avatar'
          post 'change_name', to: 'me#change_name'
          post 'change_bio', to: 'me#change_bio'
          post 'change_least_fav_color', to: 'me#change_least_fav_color'
          post 'delete_account', to: 'me#delete_account'
        end
      end

      scope :takes do
        get 'all_from_today', to: 'take#all_from_today'
        post 'create', to: 'take#create'
        post 'delete', to: 'take#delete'
        post 'like', to: 'take#like'
        post 'unlike', to: 'take#unlike'
      end
    end
  end

  mount ActionCable.server => '/cable'

  # https://medium.com/@goncalvesjoao/rails-special-route-for-single-page-applications-de9e6bf32199
  root 'root#spa'
  get '*route', to: 'root#spa', constraints: ->(request) do
    !request.xhr? && request.format.html?
  end
end
