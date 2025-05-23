module Arkham.Event.Events.JuryRig (juryRig) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Modifier

newtype JuryRig = JuryRig EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juryRig :: EventCard JuryRig
juryRig = event JuryRig Cards.juryRig

instance HasAbilities JuryRig where
  getAbilities (JuryRig a) =
    [ controlled
        a
        1
        (DuringSkillTest $ SkillTestSourceMatches $ SourceIsAsset $ assetWithAttachedEvent a)
        $ FastAbility
        $ EventUseCost (EventWithId a.id) Durability 1
    ]

instance RunMessage JuryRig where
  runMessage msg e@(JuryRig attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      assets <- getUpgradeTargets iid $ #item <> AssetControlledBy (affectsOthers $ colocatedWith iid)
      chooseOne iid [targetLabel a [PlaceEvent eid $ AttachedToAsset a Nothing] | a <- assets]
      pure . JuryRig $ attrs & tokensL . at Durability ?~ 3
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure e
    _ -> JuryRig <$> liftRunMessage msg attrs
