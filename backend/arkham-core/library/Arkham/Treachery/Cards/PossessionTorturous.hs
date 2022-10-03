module Arkham.Treachery.Cards.PossessionTorturous
  ( possessionTorturous
  , PossessionTorturous(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PossessionTorturous = PossessionTorturous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTorturous :: TreacheryCard PossessionTorturous
possessionTorturous = treachery PossessionTorturous Cards.possessionTorturous

instance HasAbilities PossessionTorturous where
  getAbilities (PossessionTorturous a) =
    [ restrictedAbility a 1 InYourHand
        $ ActionAbility Nothing (ActionCost 1 <> ResourceCost 5)
    ]

instance RunMessage PossessionTorturous where
  runMessage msg t@(PossessionTorturous attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ push $ InvestigatorKilled
        (toSource attrs)
        iid
      push $ AddTreacheryToHand iid (toId attrs)
      pure t
    EndCheckWindow{} -> case treacheryInHandOf attrs of
      Just iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ push $ InvestigatorKilled
          (toSource attrs)
          iid
        pure t
      Nothing -> pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toTarget attrs)
      pure t
    _ -> PossessionTorturous <$> runMessage msg attrs
