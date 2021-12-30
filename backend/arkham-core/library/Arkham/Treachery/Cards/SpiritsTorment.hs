module Arkham.Treachery.Cards.SpiritsTorment
  ( spiritsTorment
  , SpiritsTorment(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype SpiritsTorment = SpiritsTorment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing $ Costs
      [ActionCost 1, PlaceClueOnLocationCost 1]
    ]

instance TreacheryRunner env => RunMessage env SpiritsTorment where
  runMessage msg t@(SpiritsTorment attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ push (AttachTreachery (toId attrs) (LocationTarget lid))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> t <$ push
      (chooseOne
        iid
        [ Label
          "Take 1 horror"
          [InvestigatorAssignDamage iid source DamageAny 0 1]
        , Label "Lose 1 action" [LoseActions iid source 1]
        ]
      )
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> SpiritsTorment <$> runMessage msg attrs
