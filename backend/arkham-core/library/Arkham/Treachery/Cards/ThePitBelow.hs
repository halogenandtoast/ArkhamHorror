module Arkham.Treachery.Cards.ThePitBelow (
  thePitBelow,
  ThePitBelow (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ThePitBelow = ThePitBelow TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePitBelow :: TreacheryCard ThePitBelow
thePitBelow = treachery ThePitBelow Cards.thePitBelow

instance HasModifiersFor ThePitBelow where
  getModifiersFor (LocationTarget lid) (ThePitBelow attrs) =
    pure
      $ toModifiers attrs [ShroudModifier 1 | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities ThePitBelow where
  getAbilities (ThePitBelow a) =
    [mkAbility a 1 $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage ThePitBelow where
  runMessage msg t@(ThePitBelow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        hasThePitBelow <-
          selectAny $ treacheryAt lid <> treacheryIs Cards.thePitBelow
        pure
          $ if hasThePitBelow
            then gainSurge attrs
            else AttachTreachery (toId attrs) (toTarget lid)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <-
        select
          $ InvestigatorAt
          $ LocationWithTreachery
          $ TreacheryWithId (toId attrs)
      pushAll
        $ [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 3 0
          | iid <- iids
          ]
        <> [toDiscard (toAbilitySource attrs 1) attrs]
      pure t
    _ -> ThePitBelow <$> runMessage msg attrs
