module Arkham.Location.Cards.TheBlackCore (theBlackCore, TheBlackCore (..)) where

import Arkham.Game.Helpers (perPlayer)
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (chooseOne)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token

newtype TheBlackCore = TheBlackCore LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackCore :: LocationCard TheBlackCore
theBlackCore = location TheBlackCore Cards.theBlackCore 0 (PerPlayer 1)

instance HasModifiersFor TheBlackCore where
  getModifiersFor target (TheBlackCore attrs) | attrs `is` target = do
    n <- fieldMap LocationTokens (countTokens Depth) attrs.id
    anyClues <-
      fieldMap LocationClues (> 0) =<< selectJust (locationIs Cards.cavernsBeneathTheMoonDarkSide)
    pure $ toModifiers attrs $ ShroudModifier n : [Blocked | anyClues]
  getModifiersFor _ _ = pure []

instance HasAbilities TheBlackCore where
  getAbilities (TheBlackCore attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ RevealLocation #after Anyone $ be attrs
      , restrictedAbility attrs 2 Here actionAbility
      ]

instance RunMessage TheBlackCore where
  runMessage msg l@(TheBlackCore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 3
      placeTokens (attrs.ability 1) attrs Depth (3 + n)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseOne
        iid
        [SkillLabel s [Msg.beginSkillTest iid (attrs.ability 2) iid s 3] | s <- [#willpower, #combat]]
      pure l
    PassedThisSkillTestBy _ (isAbilitySource attrs 2 -> True) n -> do
      removeTokens (attrs.ability 2) attrs Depth n
      pure l
    _ -> TheBlackCore <$> lift (runMessage msg attrs)
