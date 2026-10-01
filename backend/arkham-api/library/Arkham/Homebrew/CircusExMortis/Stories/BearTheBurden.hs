module Arkham.Homebrew.CircusExMortis.Stories.BearTheBurden (bearTheBurden) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Placement
import Arkham.Story.Import.Lifted

{- | Destiny "burden" (:208). "Flip this card over and put it into play next to the act
deck" -- the back is the Dark of the Moon Task asset (:208b).
-}
newtype BearTheBurden = BearTheBurden StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearTheBurden :: StoryCard BearTheBurden
bearTheBurden = story BearTheBurden Cards.bearTheBurden

instance RunMessage BearTheBurden where
  runMessage msg s@(BearTheBurden attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      createAssetAt_ Assets.darkOfTheMoon NextToAct
      pure s
    _ -> BearTheBurden <$> liftRunMessage msg attrs
