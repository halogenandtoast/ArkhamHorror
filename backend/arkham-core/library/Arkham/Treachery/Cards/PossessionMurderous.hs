module Arkham.Treachery.Cards.PossessionMurderous
  ( possessionMurderous
  , PossessionMurderous(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding (TreacheryInHandOf)
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PossessionMurderous = PossessionMurderous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionMurderous :: TreacheryCard PossessionMurderous
possessionMurderous = treachery PossessionMurderous Cards.possessionMurderous

instance HasAbilities PossessionMurderous where
  getAbilities (PossessionMurderous a) =
    [ restrictedAbility a 1 InYourHand $ ActionAbility
        Nothing
        (ActionCost 1 <> InvestigatorDamageCost
          (toSource a)
          (InvestigatorAt YourLocation)
          DamageAny
          2
        )
    ]

instance RunMessage PossessionMurderous where
  runMessage msg t@(PossessionMurderous attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ push $ InvestigatorKilled
        (toSource attrs)
        iid

      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    EndCheckWindow{} -> case treacheryPlacement attrs of
      TreacheryInHandOf iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ push $ InvestigatorKilled
          (toSource attrs)
          iid
        pure t
      _ -> pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> PossessionMurderous <$> runMessage msg attrs
