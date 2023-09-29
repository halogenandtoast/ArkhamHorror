module Arkham.Treachery.Cards.PrimordialGateway (
  primordialGateway,
  PrimordialGateway (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PrimordialGateway = PrimordialGateway TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialGateway :: TreacheryCard PrimordialGateway
primordialGateway = treachery PrimordialGateway Cards.primordialGateway

instance HasModifiersFor PrimordialGateway where
  getModifiersFor (LocationTarget lid) (PrimordialGateway a) | treacheryOnLocation lid a = do
    pure $ toModifiers a [Blank]
  getModifiersFor _ _ = pure []

instance HasAbilities PrimordialGateway where
  getAbilities (PrimordialGateway x) =
    [restrictedAbility x 1 OnSameLocation $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage PrimordialGateway where
  runMessage msg t@(PrimordialGateway attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      location <- sampleLocation
      breaches <- fieldMap LocationBreaches (maybe 0 countBreaches) location
      let amount = max 0 (3 - breaches)
      pushAll
        [ AttachTreachery (toId attrs) (toTarget location)
        , PlaceBreaches (toTarget location) amount
        ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ chooseOne
          iid
          [ SkillLabel sType [beginSkillTest iid (toSource attrs) (toTarget attrs) sType 4]
          | sType <- [#intellect, #willpower]
          ]
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> PrimordialGateway <$> runMessage msg attrs
