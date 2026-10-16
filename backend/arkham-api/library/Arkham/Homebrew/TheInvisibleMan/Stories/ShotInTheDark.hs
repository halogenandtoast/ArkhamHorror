{- | "If Griffin's fight value is a set number, resolve the attack, card, or
ability.

Otherwise, if you are attacking, this attack automatically fails. If you played
a card or activated an ability, cancel each of its effects.

In either case, shuffle Shot in the Dark back into the Invisible Combat deck."

The plain "he is pinned down, so it lands" card; Pattern Recognition is the
same text under another name.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.ShotInTheDark (shotInTheDark) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype ShotInTheDark = ShotInTheDark StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotInTheDark :: StoryCard ShotInTheDark
shotInTheDark = story ShotInTheDark Cards.shotInTheDark

instance RunMessage ShotInTheDark where
  runMessage msg s@(ShotInTheDark attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      unlessM griffinsFightIsASetNumber $ theAttackFails attrs attempt
      shuffleBackIntoCombat attrs
      pure s
    _ -> ShotInTheDark <$> liftRunMessage msg attrs
