module Arkham.Homebrew.NightOfVespers.Locations.GrandBallroom (grandBallroom) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype GrandBallroom = GrandBallroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBallroom :: LocationCard GrandBallroom
grandBallroom = symbolLabel $ location GrandBallroom Cards.grandBallroom 4 (PerPlayer 2)

instance HasModifiersFor GrandBallroom where
  getModifiersFor (GrandBallroom a) = do
    unless a.revealed do
      foundARoute <- anyM remembered escapeRoutes
      unless foundARoute $ modifySelect a Anyone [CannotEnter a.id]
    whenRevealed a do
      hunters <- selectCount $ EnemyWithKeyword Keyword.Hunter
      modifySelfWhen a (hunters > 0) [ShroudModifier (-hunters)]

instance HasAbilities GrandBallroom where
  getAbilities (GrandBallroom a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #when You (be a)

instance RunMessage GrandBallroom where
  runMessage msg l@(GrandBallroom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      select (EnemyWithKeyword Keyword.Hunter) >>= traverse_ (push . HunterMove)
      pure l
    _ -> GrandBallroom <$> liftRunMessage msg attrs
