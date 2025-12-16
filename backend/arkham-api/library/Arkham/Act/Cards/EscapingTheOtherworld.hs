module Arkham.Act.Cards.EscapingTheOtherworld (escapingTheOtherworld) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (AssetDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Outsider))

newtype EscapingTheOtherworld = EscapingTheOtherworld ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapingTheOtherworld :: ActCard EscapingTheOtherworld
escapingTheOtherworld = act (3, A) EscapingTheOtherworld Cards.escapingTheOtherworld Nothing

instance HasModifiersFor EscapingTheOtherworld where
  getModifiersFor (EscapingTheOtherworld a) =
    modifySelect a (EnemyWithTrait Outsider) [RemoveKeyword Keyword.Aloof]

instance HasAbilities EscapingTheOtherworld where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ forced $ AssetDefeated #when ByAny (assetIs Assets.theRedGlovedManHeWasAlwaysThere)
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage EscapingTheOtherworld where
  runMessage msg a@(EscapingTheOtherworld attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- noop, will automatically occur
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      aliki <-
        selectAny
          $ VictoryDisplayCardMatch
          $ basic
          $ cardIs Assets.alikiZoniUperetriaTheMaidWithTheScarletSash
      theRedGlovedMan <-
        selectAny
          $ VictoryDisplayCardMatch
          $ basic
          $ cardIs Assets.theRedGlovedManHeWasAlwaysThere

      push $ case (aliki, theRedGlovedMan) of
        (True, True) -> R1
        (False, True) -> R2
        (True, False) -> R3
        (False, False) -> R4
      pure a
    _ -> EscapingTheOtherworld <$> liftRunMessage msg attrs
