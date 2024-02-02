module Arkham.Location.Cards.NamelessRuins (namelessRuins, NamelessRuins (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype NamelessRuins = NamelessRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

namelessRuins :: LocationCard NamelessRuins
namelessRuins = location NamelessRuins Cards.namelessRuins 5 (PerPlayer 1)

instance HasAbilities NamelessRuins where
  getAbilities (NamelessRuins attrs) =
    veiled
      attrs
      [mkAbility attrs 1 $ forced $ TurnEnds #when (You <> at_ (be attrs) <> HasMatchingAsset #ally)]

instance RunMessage NamelessRuins where
  runMessage msg l@(NamelessRuins attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.whatRemainsOfTyrrhia
      pure . NamelessRuins $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      allies <- selectList $ assetControlledBy iid <> #ally
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel ally [AssetDamage ally (attrs.ability 1) 1 0] | ally <- allies]
      pure l
    _ -> NamelessRuins <$> runMessage msg attrs
