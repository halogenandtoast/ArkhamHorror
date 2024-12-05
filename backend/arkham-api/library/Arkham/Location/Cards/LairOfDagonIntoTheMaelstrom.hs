module Arkham.Location.Cards.LairOfDagonIntoTheMaelstrom (
  lairOfDagonIntoTheMaelstrom,
  LairOfDagonIntoTheMaelstrom (..),
)
where

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.IntoTheMaelstrom.Helpers
import Arkham.Trait (Trait (Sanctum))

newtype LairOfDagonIntoTheMaelstrom = LairOfDagonIntoTheMaelstrom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfDagonIntoTheMaelstrom :: LocationCard LairOfDagonIntoTheMaelstrom
lairOfDagonIntoTheMaelstrom =
  locationWith
    LairOfDagonIntoTheMaelstrom
    Cards.lairOfDagonIntoTheMaelstrom
    6
    (PerPlayer 3)
    connectsToAdjacent

instance HasModifiersFor LairOfDagonIntoTheMaelstrom where
  getModifiersFor (LairOfDagonIntoTheMaelstrom a) = modifySelfMaybe a do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    guard $ n > 0
    pure [ShroudModifier (-n)]

instance HasAbilities LairOfDagonIntoTheMaelstrom where
  getAbilities (LairOfDagonIntoTheMaelstrom a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted
            a
            1
            ( foldMap
                (exists . LocationWithKey)
                [BlueKey, RedKey, GreenKey, YellowKey, PurpleKey, WhiteKey, BlackKey]
                <> HasRecord TheOrdersRitualWasDisrupted
            )
          $ FastAbility Free
      ]

instance RunMessage LairOfDagonIntoTheMaelstrom where
  runMessage msg l@(LairOfDagonIntoTheMaelstrom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flashback iid Flashback14
      pure l
    _ -> LairOfDagonIntoTheMaelstrom <$> liftRunMessage msg attrs
