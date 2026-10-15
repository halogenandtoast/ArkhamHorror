module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.ThePhantom (thePhantom) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (keepConcealedInPlay, resolveConcealed)
import Arkham.Campaigns.TheScarletKeys.Helpers (afterExposed)
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (hasModifier, modified_)
import Arkham.Homebrew.ThePhantomOfTheOpera.Acts.MasksUponMasks (mayIgnorePhantomForced)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype ThePhantom = ThePhantom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantom :: EnemyCard ThePhantom
thePhantom = enemy ThePhantom Cards.thePhantom

{- | "Do not remove other concealed mini-cards from play while resolving this effect." He is out of
the shadows only between his exposure and his Forced returning him, which is when the sweep in
@DoStep 2 (Flip ...)@ would otherwise fire, ahead of any after-expose window.
-}
instance HasModifiersFor ThePhantom where
  getModifiersFor (ThePhantom a) =
    when (isInPlayPlacement a.placement && a.placement /= InTheShadows)
      $ modified_ a ScenarioTarget [keepConcealedInPlay]

instance HasAbilities ThePhantom where
  getAbilities (ThePhantom a) = extend1 a $ mkAbility a 1 $ forced $ afterExposed a

instance RunMessage ThePhantom where
  runMessage msg e@(ThePhantom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Masks Upon Masks lets the investigators decline this, which is the only way he dies.
      hasModifier attrs mayIgnorePhantomForced >>= \case
        False -> doStep 1 msg
        True -> chooseOneM iid $ scenarioI18n $ scope "thePhantom" do
          labeled "returnToTheShadows" $ doStep 1 msg
          labeled "ignoreForcedEffect" nothing
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      place attrs InTheShadows
      resolveConcealed iid attrs.id
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let tragicFigure = lookupCard Cards.thePhantomTragicFigure attrs.cardId
      push $ ReplaceEnemy attrs.id tragicFigure Swap
      pure e
    _ -> ThePhantom <$> liftRunMessage msg attrs
