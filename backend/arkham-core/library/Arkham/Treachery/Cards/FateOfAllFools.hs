module Arkham.Treachery.Cards.FateOfAllFools (
  fateOfAllFools,
  FateOfAllFools (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FateOfAllFools = FateOfAllFools TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFools :: TreacheryCard FateOfAllFools
fateOfAllFools = treachery FateOfAllFools Cards.fateOfAllFools

instance RunMessage FateOfAllFools where
  runMessage msg t@(FateOfAllFools attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mAlreadyInPlay <-
        selectOne $
          treacheryIs Cards.fateOfAllFools
            <> TreacheryInThreatAreaOf Anyone
      case mAlreadyInPlay of
        Just tid -> do
          iid' <- selectJust $ HasMatchingTreachery $ TreacheryWithId tid
          push $
            chooseOne
              iid
              [ Label
                  "An investigator with another copy of Fate of All Fools in his or her threat area takes 2 direct damage."
                  [InvestigatorDirectDamage iid' (toSource attrs) 2 0]
              , Label
                  "Place 1 doom on another copy of Fate of All Fools."
                  [PlaceDoom (toSource attrs) (TreacheryTarget tid) 1]
              ]
        Nothing -> push $ AttachTreachery (toId attrs) (InvestigatorTarget iid)
      pure t
    _ -> FateOfAllFools <$> runMessage msg attrs
