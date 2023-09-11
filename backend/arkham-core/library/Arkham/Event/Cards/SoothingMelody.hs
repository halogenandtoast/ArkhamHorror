module Arkham.Event.Cards.SoothingMelody (
  soothingMelody,
  SoothingMelody (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Damage
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message

newtype SoothingMelody = SoothingMelody EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soothingMelody :: EventCard SoothingMelody
soothingMelody =
  event SoothingMelody Cards.soothingMelody

instance RunMessage SoothingMelody where
  runMessage msg e@(SoothingMelody attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      canDraw <- iid <=~> InvestigatorCanDrawCards Anyone
      drawing <- drawCards iid attrs 1
      pushAll $ ResolveEvent iid eid Nothing [] : [drawing | canDraw]
      pure e
    ResolveEvent iid eid mHealed _ | eid == toId attrs -> do
      damageInvestigators <-
        selectList $ HealableInvestigator (toSource attrs) DamageType $ InvestigatorAt YourLocation
      horrorInvestigators <-
        selectList $ HealableInvestigator (toSource attrs) HorrorType $ InvestigatorAt YourLocation
      damageAssets <-
        selectListMap AssetTarget
          $ HealableAsset (toSource attrs) DamageType
          $ AssetAt YourLocation <> AllyAsset
      horrorAssets <-
        selectListMap AssetTarget
          $ HealableAsset (toSource attrs) HorrorType
          $ AssetAt YourLocation <> AllyAsset

      let
        componentLabel component target = case target of
          InvestigatorTarget iid' ->
            ComponentLabel (InvestigatorComponent iid' component)
          AssetTarget aid -> ComponentLabel (AssetComponent aid component)
          _ -> error "unhandled target"
        investigatorDamageChoices =
          [ componentLabel DamageToken (toTarget i)
            $ HealDamage (toTarget i) (toSource attrs) 1
              : [ResolveEvent iid eid (Just $ toTarget i) [] | isNothing mHealed]
          | i <- damageInvestigators
          ]
        damageAssetChoices =
          [ componentLabel DamageToken asset
            $ HealDamage asset (toSource attrs) 1
              : [ResolveEvent iid eid (Just $ toTarget asset) [] | isNothing mHealed]
          | asset <- damageAssets
          ]
        horrorAssetChoices =
          [ componentLabel HorrorToken asset
            $ HealHorror asset (toSource attrs) 1
              : [ResolveEvent iid eid (Just $ toTarget asset) [] | isNothing mHealed]
          | asset <- horrorAssets
          ]

      investigatorHorrorChoices <- for horrorInvestigators $ \i -> do
        healHorror <- fromJustNote "should be healable" <$> getHealHorrorMessage attrs 1 i
        pure $ componentLabel HorrorToken (toTarget i) [healHorror]

      let choices =
            investigatorDamageChoices <> investigatorHorrorChoices <> damageAssetChoices <> horrorAssetChoices

      unless (null choices) $ push $ chooseOne iid choices
      pure e
    _ -> SoothingMelody <$> runMessage msg attrs
