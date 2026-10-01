module Arkham.Homebrew.CircusExMortis.Stories.ReciteThePrayer (reciteThePrayer) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Placement
import Arkham.Story.Import.Lifted

{- | Destiny "prayer" (:207). "Flip this card over and put it into play next to the act
deck" -- the back is the Diana's Blessing Task asset (:207b). Next to the act deck is
'NextToAct': in play, controlled by nobody, and at no location, which is why the asset's
own first line has to grant its seat access.
-}
newtype ReciteThePrayer = ReciteThePrayer StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reciteThePrayer :: StoryCard ReciteThePrayer
reciteThePrayer = story ReciteThePrayer Cards.reciteThePrayer

instance RunMessage ReciteThePrayer where
  runMessage msg s@(ReciteThePrayer attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      createAssetAt_ Assets.dianasBlessing NextToAct
      pure s
    _ -> ReciteThePrayer <$> liftRunMessage msg attrs
