module Arkham.Event.Events.SoothingMelody (soothingMelody, SoothingMelody (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype SoothingMelody = SoothingMelody EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soothingMelody :: EventCard SoothingMelody
soothingMelody = event SoothingMelody Cards.soothingMelody

instance RunMessage SoothingMelody where
  runMessage msg e@(SoothingMelody attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ ResolveEventChoice iid eid 1 Nothing []
      drawCardsIfCan iid attrs 1
      pure e
    ResolveEventChoice iid eid n _ _ | eid == toId attrs -> do
      modifiers' <- liftA2 (<>) (getModifiers (toCardId attrs)) (getModifiers attrs)
      let
        updateLimit :: Int -> ModifierType -> Int
        updateLimit x (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "use3" o of
            Just (Success True) -> 3
            _ -> x
        updateLimit x _ = x

      let limit = foldl' updateLimit 2 modifiers'
      let location = locationWithInvestigator iid
      damageInvestigators <- select $ HealableInvestigator (toSource attrs) #damage $ at_ location
      horrorInvestigators <- select $ HealableInvestigator (toSource attrs) #horror $ at_ location
      damageAssets <-
        selectTargets
          $ HealableAsset (toSource attrs) #damage
          $ at_ location
          <> #ally
          <> AssetControlledBy (affectsOthers Anyone)
      horrorAssets <-
        selectTargets
          $ HealableAsset (toSource attrs) #horror
          $ at_ location
          <> #ally
          <> AssetControlledBy (affectsOthers Anyone)

      let
        componentLabel component target = case target of
          InvestigatorTarget iid' -> ComponentLabel (InvestigatorComponent iid' component)
          AssetTarget aid -> ComponentLabel (AssetComponent aid component)
          _ -> error "unhandled target"
        investigatorDamageChoices =
          [ componentLabel DamageToken (toTarget i)
            $ HealDamage (toTarget i) (toSource attrs) 1
            : [ResolveEventChoice iid eid (n + 1) Nothing [] | n < limit]
          | i <- damageInvestigators
          ]
        damageAssetChoices =
          [ componentLabel DamageToken asset
            $ HealDamage asset (toSource attrs) 1
            : [ResolveEventChoice iid eid (n + 1) Nothing [] | n < limit]
          | asset <- damageAssets
          ]
        horrorAssetChoices =
          [ componentLabel HorrorToken asset
            $ HealHorror asset (toSource attrs) 1
            : [ResolveEventChoice iid eid (n + 1) Nothing [] | n < limit]
          | asset <- horrorAssets
          ]

      investigatorHorrorChoices <- for horrorInvestigators $ \i -> do
        pure $ componentLabel HorrorToken (toTarget i) $ HealHorror (toTarget i) (toSource attrs) 1
          : [ResolveEventChoice iid eid (n + 1) Nothing [] | n < limit]

      let choices =
            investigatorDamageChoices <> investigatorHorrorChoices <> damageAssetChoices <> horrorAssetChoices

      unless (null choices) $ chooseOne iid choices
      pure e
    _ -> SoothingMelody <$> liftRunMessage msg attrs
