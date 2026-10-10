module Arkham.Homebrew.AgesUnwound.Locations.HedgeMaze (hedgeMaze) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Window (getBatchId)

newtype HedgeMaze = HedgeMaze LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: LocationCard HedgeMaze
hedgeMaze = symbolLabel $ location HedgeMaze Cards.hedgeMaze 5 (PerPlayer 1)

instance HasAbilities HedgeMaze where
  getAbilities (HedgeMaze a) =
    extendRevealed
      a
      [ -- "Forced - At the end of your turn: Test [willpower] (3). If you fail,
        -- place 1 resource on Hedge Maze (from the token pool)."
        restricted a 1 Here $ forced $ TurnEnds #when You
      , -- "Forced - When you would leave Hedge Maze, if there is at least 1
        -- resource on it: Remove 1 resource from Hedge Maze and cancel the
        -- effects of the move."
        restricted a 2 (thisExists a $ LocationWithResources $ atLeast 1)
          $ forced
          $ WouldMove #when You #any (be a) Anywhere
      ]

instance RunMessage HedgeMaze where
  runMessage msg l@(HedgeMaze attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs Token.Resource 1
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getBatchId -> batchId) _ -> do
      removeTokens (attrs.ability 2) attrs Token.Resource 1
      cancelMovement (attrs.ability 2) iid
      cancelBatch batchId
      pure l
    _ -> HedgeMaze <$> liftRunMessage msg attrs
