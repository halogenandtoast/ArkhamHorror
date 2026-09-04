module Arkham.Homebrew.DarkMatter.Stories.Delights (delights) where

import Arkham.ChaosBag.Base (chaosBagChaosTokens)
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getChaosBag)
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Message.Lifted.Choose
import Arkham.Story.Import.Lifted

newtype Delights = Delights StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delights :: StoryCard Delights
delights = story Delights Cards.delights

{- | "You may choose and remove any non-[auto_fail] symbol token from the chaos
bag for the remainder of the campaign. Add this card to the victory display."
-}
removableFaces :: [ChaosTokenFace]
removableFaces = [Skull, Cultist, Tablet, ElderThing, ElderSign]

instance RunMessage Delights where
  runMessage msg s@(Delights attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      faces <- filter (`elem` removableFaces) . map chaosTokenFace . chaosBagChaosTokens <$> getChaosBag
      unless (null faces) do
        chooseOneM iid $ campaignI18n do
          labeled "delights.doNotRemoveAToken" nothing
          for_ (ordNub faces) \face ->
            labeled ("delights.remove" <> tshow face) $ push $ RemoveChaosToken face
      addToVictory iid attrs
      pure s
    _ -> Delights <$> liftRunMessage msg attrs
