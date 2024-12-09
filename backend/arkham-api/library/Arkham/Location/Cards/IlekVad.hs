module Arkham.Location.Cards.IlekVad (ilekVad, IlekVad (..)) where

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.History
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait (Relic, Ritual, Spell))

newtype IlekVad = IlekVad LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ilekVad :: LocationCard IlekVad
ilekVad = location IlekVad Cards.ilekVad 2 (PerPlayer 1)

instance HasModifiersFor IlekVad where
  getModifiersFor (IlekVad a) = modifySelectMaybe a (investigatorAt a) \iid -> do
    playedCards <- lift $ historyPlayedCards <$> getHistory RoundHistory iid
    let cardMatcher = mapOneOf CardWithTrait [Spell, Ritual, Relic]
    guard $ none (`cardMatch` cardMatcher) playedCards
    pure [ReduceCostOf cardMatcher 1]

instance HasAbilities IlekVad where
  getAbilities (IlekVad attrs) = veiled attrs []

instance RunMessage IlekVad where
  runMessage msg (IlekVad attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.thePalaceOfRainbows
      pure . IlekVad $ attrs & canBeFlippedL .~ False
    _ -> IlekVad <$> liftRunMessage msg attrs
