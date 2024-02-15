module Arkham.Event.Cards.SoothingMelody (
  soothingMelody,
  SoothingMelody (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Damage
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype SoothingMelody = SoothingMelody EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soothingMelody :: EventCard SoothingMelody
soothingMelody =
  event SoothingMelody Cards.soothingMelody

instance RunMessage SoothingMelody where
  runMessage msg e@(SoothingMelody attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCardsIfCan iid attrs 1
      pushAll $ ResolveEventChoice iid eid 1 Nothing [] : toList drawing
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
      damageInvestigators <-
        select $ HealableInvestigator (toSource attrs) DamageType $ InvestigatorAt YourLocation
      horrorInvestigators <-
        select $ HealableInvestigator (toSource attrs) HorrorType $ InvestigatorAt YourLocation
      damageAssets <-
        selectMap AssetTarget
          $ HealableAsset (toSource attrs) DamageType
          $ AssetAt YourLocation
          <> AllyAsset
          <> AssetControlledBy (affectsOthers Anyone)
      horrorAssets <-
        selectMap AssetTarget
          $ HealableAsset (toSource attrs) HorrorType
          $ AssetAt YourLocation
          <> AllyAsset
          <> AssetControlledBy (affectsOthers Anyone)

      let
        componentLabel component target = case target of
          InvestigatorTarget iid' ->
            ComponentLabel (InvestigatorComponent iid' component)
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
        healHorror <- fromJustNote "should be healable" <$> getHealHorrorMessage attrs 1 i
        pure $ componentLabel HorrorToken (toTarget i) [healHorror]

      let choices =
            investigatorDamageChoices <> investigatorHorrorChoices <> damageAssetChoices <> horrorAssetChoices

      player <- getPlayer iid
      unless (null choices) $ push $ chooseOne player choices
      pure e
    _ -> SoothingMelody <$> runMessage msg attrs
