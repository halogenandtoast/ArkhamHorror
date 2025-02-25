module Arkham.Asset.Assets.Misdirection2 (misdirection2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Location
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Misdirection2 = Misdirection2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

misdirection2 :: AssetCard Misdirection2
misdirection2 = asset Misdirection2 Cards.misdirection2

instance HasAbilities Misdirection2 where
  getAbilities (Misdirection2 a) =
    [ restricted a 1 ControlsThis
        $ ConstantReaction
          "Trigger sealed keyword again"
          (DiscoveringLastClue #after You YourLocation)
          (SealCost $ oneOf [#"0", #"+1"])
    , restricted a 2 ControlsThis
        $ triggered
          ( EnemyAttacks
              #when
              (affectsOthers $ InvestigatorAt YourLocation)
              (CancelableEnemyAttack AnyEnemyAttack)
              AnyEnemy
          )
          (exhaust a <> ReleaseChaosTokensCost 1 #any)
    ]

instance RunMessage Misdirection2 where
  runMessage msg a@(Misdirection2 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let details = (getAttackDetails ws)
      cancelAttack attrs details
      for_ details.target.investigator \iid -> do
        locations <- getAccessibleLocations iid (attrs.ability 1)
        unless (null locations) do
          chooseOneM iid do
            labeled "Do not move" nothing
            targets locations (moveTo (attrs.ability 1) iid)
      pure a
    _ -> Misdirection2 <$> liftRunMessage msg attrs
