module Arkham.Event.Cards.BloodRite (
  bloodRite,
  BloodRite (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Projection
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

bloodRiteLimit :: HasGame m => EventAttrs -> m Int
bloodRiteLimit attrs = do
  modifiers' <- liftA2 (<>) (getModifiers (toCardId attrs)) (getModifiers attrs)
  let
    updateLimit :: Int -> ModifierType -> Int
    updateLimit x (MetaModifier (Object o)) =
      case fromJSON <$> KeyMap.lookup "use3" o of
        Just (Success True) -> 3
        _ -> x
    updateLimit x _ = x

  pure $ foldl' updateLimit 2 modifiers'

instance RunMessage BloodRite where
  runMessage msg e@(BloodRite attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == eventId -> do
      limit <- bloodRiteLimit attrs
      drawing <- drawCards iid attrs limit
      pushAll
        [ drawing
        , PayForCardAbility iid (EventSource eid) windows 1 (DiscardCardPayment [])
        ]
      pure e
    PayForCardAbility iid source windows 1 payment@(DiscardCardPayment discardedCards) | isSource attrs source -> do
      limit <- bloodRiteLimit attrs
      if length discardedCards == limit
        then push (UseCardAbility iid source 1 windows payment)
        else do
          cards <- fieldMap InvestigatorHand (filter isDiscardable) iid
          player <- getPlayer iid
          push
            $ chooseOne player
            $ [ targetLabel
                (toCardId card)
                [ DiscardCard iid (toSource attrs) (toCardId card)
                , PayForCardAbility iid source windows 1 (DiscardCardPayment $ card : discardedCards)
                ]
              | card <- cards
              ]
            <> [ Label
                  ( "Continue having discarded "
                      <> tshow (length discardedCards)
                      <> " cards"
                  )
                  [UseCardAbility iid source 1 windows payment]
               ]
      pure e
    UseCardAbility iid source 1 _ (DiscardCardPayment xs) | isSource attrs source -> do
      enemyIds <- selectList $ enemyAtLocationWith iid
      canDealDamage <- withoutModifier iid CannotDealDamage
      player <- getPlayer iid
      pushAll
        $ replicate (length xs)
        $ chooseOne player
        $ [Label "Gain Resource" [TakeResources iid 1 (toAbilitySource attrs 1) False]]
        <> [ Label "Spend Resource and Deal 1 Damage To Enemy At Your Location"
            $ [ SpendResources iid 1
              , chooseOne
                  player
                  [targetLabel enemyId [EnemyDamage enemyId $ nonAttack source 1] | enemyId <- enemyIds]
              ]
           | canDealDamage
           , notNull enemyIds
           ]
      pure e
    _ -> BloodRite <$> runMessage msg attrs
