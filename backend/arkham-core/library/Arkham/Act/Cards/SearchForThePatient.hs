module Arkham.Act.Cards.SearchForThePatient (
  SearchForThePatient (..),
  searchForThePatient,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher

newtype SearchForThePatient = SearchForThePatient ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

searchForThePatient :: ActCard SearchForThePatient
searchForThePatient = act (2, A) SearchForThePatient Cards.searchForThePatient Nothing

instance HasAbilities SearchForThePatient where
  getAbilities (SearchForThePatient x) =
    [ mkAbility x 1
        $ Objective
        $ ForcedAbility
        $ TookControlOfAsset #when You
        $ assetIs Assets.randolphCarterChainedToTheWakingWorld
    ]

instance RunMessage SearchForThePatient where
  runMessage msg a@(SearchForThePatient attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      pushAll [ShuffleEncounterDiscardBackIn, advanceActDeck attrs]
      pure a
    _ -> SearchForThePatient <$> runMessage msg attrs
