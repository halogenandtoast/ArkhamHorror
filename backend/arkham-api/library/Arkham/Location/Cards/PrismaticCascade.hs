module Arkham.Location.Cards.PrismaticCascade (prismaticCascade, PrismaticCascade (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards (prismaticCascade)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Name
import Control.Monad.Extra (findM)

newtype PrismaticCascade = PrismaticCascade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticCascade :: LocationCard PrismaticCascade
prismaticCascade = location PrismaticCascade Cards.prismaticCascade 2 (Static 3)

instance HasAbilities PrismaticCascade where
  getAbilities (PrismaticCascade attrs) =
    extendRevealed1 attrs
      $ mkAbility attrs 1
      $ forced
      $ DiscoveringLastClue #after Anyone (be attrs)

instance RunMessage PrismaticCascade where
  runMessage msg l@(PrismaticCascade attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      randomDiscard iid attrs
      let labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . PrismaticCascade $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure l
    _ -> PrismaticCascade <$> liftRunMessage msg attrs
