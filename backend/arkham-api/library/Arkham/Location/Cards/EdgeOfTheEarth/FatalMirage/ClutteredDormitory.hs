module Arkham.Location.Cards.EdgeOfTheEarth.FatalMirage.ClutteredDormitory (clutteredDormitory) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Stories

newtype ClutteredDormitory = ClutteredDormitory LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clutteredDormitory :: LocationCard ClutteredDormitory
clutteredDormitory = location ClutteredDormitory Cards.clutteredDormitory 4 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAnUnspeakableEvil]

instance HasModifiersFor ClutteredDormitory where
  getModifiersFor (ClutteredDormitory a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [Assets.danforthBrilliantStudent, Assets.danforthBrilliantStudentResolute]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    clearedOfMirages a mirageCards

instance HasAbilities ClutteredDormitory where
  getAbilities (ClutteredDormitory a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , restricted a 1 (youExist $ at_ (be a))
          $ forced
          $ ResolvingRevelation #when You
          $ TreacheryWithTitle "Tekeli-li"
      ]

instance RunMessage ClutteredDormitory where
  runMessage msg (ClutteredDormitory attrs) = runQueueT $ case msg of
    _ -> ClutteredDormitory <$> mirageRunner Stories.clutteredDormitory mirageCards 2 msg attrs
