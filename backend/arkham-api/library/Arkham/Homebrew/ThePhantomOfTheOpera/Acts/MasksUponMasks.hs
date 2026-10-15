module Arkham.Homebrew.ThePhantomOfTheOpera.Acts.MasksUponMasks (masksUponMasks, mayIgnorePhantomForced) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype MasksUponMasks = MasksUponMasks ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masksUponMasks :: ActCard MasksUponMasks
masksUponMasks = act (4, A) MasksUponMasks Cards.masksUponMasks Nothing

{- | "While there is horror on 2 or more locations, you may ignore the forced effect on the
Phantom." The Phantom's own Forced reads this and offers the choice.
-}
mayIgnorePhantomForced :: ModifierType
mayIgnorePhantomForced = ScenarioModifier "thePhantomOfTheOpera.mayIgnorePhantomForced"

instance HasModifiersFor MasksUponMasks where
  getModifiersFor (MasksUponMasks a) = when (onSide A a) do
    horrified <- selectCount $ LocationWithHorror (atLeast 1)
    when (horrified >= 2) $ modifySelect a (enemyIs Enemies.thePhantom) [mayIgnorePhantomForced]

enteredLocation :: [Window] -> Maybe LocationId
enteredLocation = \case
  (windowType -> Window.EnemyEnters _ lid) : _ -> Just lid
  _ : rest -> enteredLocation rest
  [] -> Nothing

instance HasAbilities MasksUponMasks where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ SetAsideCardMatch $ cardIs Assets.christineDaae)
        $ forced
        $ EnemyEnters #when Anywhere (enemyIs Enemies.thePhantom)
    , mkAbility a 2 $ Objective $ forced $ ifEnemyDefeated Enemies.thePhantom
    ]

instance RunMessage MasksUponMasks where
  runMessage msg a@(MasksUponMasks attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (enteredLocation -> Just lid) _ -> do
      christine <- getSetAsideCard Assets.christineDaae
      createAssetAt_ christine (AtLocation lid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      tragicFigure <- selectAny $ enemyIs Enemies.thePhantomTragicFigure
      push $ if tragicFigure then R4 else R3
      pure a
    _ -> MasksUponMasks <$> liftRunMessage msg attrs
