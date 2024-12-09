module Arkham.Treachery.Cards.PrimordialGateway (primordialGateway, PrimordialGateway (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PrimordialGateway = PrimordialGateway TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialGateway :: TreacheryCard PrimordialGateway
primordialGateway = treachery PrimordialGateway Cards.primordialGateway

instance HasModifiersFor PrimordialGateway where
  getModifiersFor (PrimordialGateway a) = case a.placement of
    AttachedToLocation lid -> modified_ a lid [Blank]
    _ -> pure mempty

instance HasAbilities PrimordialGateway where
  getAbilities (PrimordialGateway x) =
    [skillTestAbility $ restrictedAbility x 1 OnSameLocation $ ActionAbility [] $ ActionCost 1]

instance RunMessage PrimordialGateway where
  runMessage msg t@(PrimordialGateway attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      location <- sampleLocation
      breaches <- fieldMap LocationBreaches (maybe 0 countBreaches) location
      let amount = max 0 (3 - breaches)
      attachTreachery attrs location
      push $ PlaceBreaches (toTarget location) amount
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      chooseOne
        iid
        [ SkillLabel sType [Msg.beginSkillTest sid iid (attrs.ability 1) (toTarget attrs) sType (Fixed 4)]
        | sType <- [#intellect, #willpower]
        ]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PrimordialGateway <$> liftRunMessage msg attrs
