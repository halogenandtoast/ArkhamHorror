module Arkham.Asset.Assets.Obscure2 (obscure2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealLocation)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Window (engagedEnemy)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Obscure2 = Obscure2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscure2 :: AssetCard Obscure2
obscure2 = asset Obscure2 Cards.obscure2

instance HasAbilities Obscure2 where
  getAbilities (Obscure2 a) =
    [ restricted a 1 ControlsThis
        $ ConstantReaction
          "Trigger seal again"
          (oneOf [RevealLocation #after You Anywhere, PutLocationIntoPlay #after You Anywhere])
          (SealCost #"0")
    , restricted a 2 ControlsThis
        $ triggered
          (EnemyEngaged #when You $ oneOf [CanFightEnemy (a.ability 2), EnemyCanBeEvadedBy (a.ability 2)])
          (exhaust a <> ReleaseChaosTokensCost 1 #any)
    ]

instance RunMessage Obscure2 where
  runMessage msg a@(Obscure2 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (engagedEnemy -> eid) _ -> do
      canFight <- matches eid (CanFightEnemy (attrs.ability 2))
      canEvade <- matches eid (EnemyCanBeEvadedBy (attrs.ability 2))
      sid <- getRandom

      n <-
        fromMaybe 0 <$> runMaybeT do
          lid <- MaybeT $ getMaybeLocation iid
          MaybeT $ field LocationShroud lid

      skillTestModifier sid (attrs.ability 2) iid (BaseSkill n)

      chooseOneM iid do
        when canEvade $ evading eid do
          chooseEvadeEnemyMatch sid iid (attrs.ability 2) (EnemyWithId eid)
        when canFight $ fighting eid do
          chooseFightEnemyMatch sid iid (attrs.ability 2) (EnemyWithId eid)
      pure a
    _ -> Obscure2 <$> liftRunMessage msg attrs
