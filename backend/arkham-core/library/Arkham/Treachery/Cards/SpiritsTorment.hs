module Arkham.Treachery.Cards.SpiritsTorment (
  spiritsTorment,
  SpiritsTorment (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SpiritsTorment = SpiritsTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

spiritsTorment :: TreacheryCard SpiritsTorment
spiritsTorment = treachery SpiritsTorment Cards.spiritsTorment

instance HasAbilities SpiritsTorment where
  getAbilities (SpiritsTorment a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ Leaves Timing.When You
        $ LocationWithTreachery
        $ TreacheryWithId
        $ toId a
    , restrictedAbility a 2 OnSameLocation
        $ ActionAbility []
        $ Costs
          [ActionCost 1, PlaceClueOnLocationCost 1]
    ]

instance RunMessage SpiritsTorment where
  runMessage msg t@(SpiritsTorment attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid
        $ \lid -> push $ AttachTreachery (toId attrs) (LocationTarget lid)
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      hasActions <- member iid <$> select InvestigatorWithAnyActionsRemaining
      player <- getPlayer iid
      push
        $ if hasActions
          then
            chooseOne
              player
              [ Label "Take 1 horror" [assignHorror iid source 1]
              , Label "Lose 1 action" [LoseActions iid source 1]
              ]
          else InvestigatorAssignDamage iid source DamageAny 0 1
      pure t
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 2) attrs)
    _ -> SpiritsTorment <$> runMessage msg attrs
