{- | Act 3, Taste of Your Own Medicine.

The investigators have to put Hyde down while he is the one in the body, and he
is at his worst here. The moment Jekyll is back -- the parley on Hyde's own card
is the only way that happens -- the act advances itself and they win.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.TasteOfYourOwnMedicine (
  tasteOfYourOwnMedicine,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher

newtype TasteOfYourOwnMedicine = TasteOfYourOwnMedicine ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tasteOfYourOwnMedicine :: ActCard TasteOfYourOwnMedicine
tasteOfYourOwnMedicine = act (3, A) TasteOfYourOwnMedicine Cards.tasteOfYourOwnMedicine Nothing

-- | "Edward Hyde gets +1 fight, +1 evade, and gains retaliate and alert."
instance HasModifiersFor TasteOfYourOwnMedicine where
  getModifiersFor (TasteOfYourOwnMedicine a) = when (onSide A a) do
    modifySelect a edwardHyde [EnemyFight 1, EnemyEvade 1, #retaliate, #alert]

-- | "__Objective__ - If Henry Jekyll is in play, you must immediately advance."
instance HasAbilities TasteOfYourOwnMedicine where
  getAbilities = actAbilities1 \a ->
    scenarioI18n
      $ withI18nTooltip "tasteOfYourOwnMedicine.objective"
      $ restricted a 1 (exists henryJekyll)
      $ Objective
      $ forced AnyWindow

instance RunMessage TasteOfYourOwnMedicine where
  runMessage msg a@(TasteOfYourOwnMedicine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TasteOfYourOwnMedicine <$> liftRunMessage msg attrs
