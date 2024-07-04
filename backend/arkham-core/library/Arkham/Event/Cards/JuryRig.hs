module Arkham.Event.Cards.JuryRig (juryRig, JuryRig (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement

newtype JuryRig = JuryRig EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juryRig :: EventCard JuryRig
juryRig = event JuryRig Cards.juryRig

instance HasAbilities JuryRig where
  getAbilities (JuryRig a) =
    [ controlledAbility
        a
        1
        ( DuringSkillTest $ SkillTestSourceMatches $ SourceIsAsset $ AssetWithAttachedEvent $ EventWithId a.id
        )
        $ FastAbility
        $ EventUseCost (EventWithId a.id) Durability 1
    ]

instance RunMessage JuryRig where
  runMessage msg e@(JuryRig attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      assets <- select $ #item <> AssetControlledBy (affectsOthers $ colocatedWith iid)
      chooseOne iid [targetLabel a [PlaceEvent iid eid $ AttachedToAsset a Nothing] | a <- assets]
      pure . JuryRig $ attrs & usesL . at Durability .~ Just 3
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 2)
      pure e
    _ -> JuryRig <$> liftRunMessage msg attrs
