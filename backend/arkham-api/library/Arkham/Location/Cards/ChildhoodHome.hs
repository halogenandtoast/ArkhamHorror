module Arkham.Location.Cards.ChildhoodHome (childhoodHome) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Token qualified as Token

newtype ChildhoodHome = ChildhoodHome LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childhoodHome :: LocationCard ChildhoodHome
childhoodHome = location ChildhoodHome Cards.childhoodHome 2 (PerPlayer 2)

instance HasAbilities ChildhoodHome where
  getAbilities (ChildhoodHome a) =
    extendRevealed
      a
      [ restricted
          a
          1
          ( Here
              <> Remembered ThomasAndMaryHaveMet
              <> exists (AssetWithTitle "Thomas Corrigan" <> AssetAt (be a))
              <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (be a))
          )
          actionAbility
      , groupLimit PerGame
          $ restricted a 2 (Here <> Remembered CorriganIndustriesHasBeenFounded)
          $ ActionAbility [] (ActionCost 2)
      ]

instance RunMessage ChildhoodHome where
  runMessage msg l@(ChildhoodHome attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember ThomasAndMaryHaveMarried
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      corrigan <- selectOne $ locationIs Cards.corriganIndustries
      for_ corrigan \corrigan' ->
        placeTokens (attrs.ability 2) corrigan' Token.TimeCapsule 1
      pure l
    _ -> ChildhoodHome <$> liftRunMessage msg attrs
