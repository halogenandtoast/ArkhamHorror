module Arkham.Homebrew.DarkMatter.Acts.SaveOurSouls (saveOurSouls) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype SaveOurSouls = SaveOurSouls ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saveOurSouls :: ActCard SaveOurSouls
saveOurSouls = act (2, A) SaveOurSouls Cards.saveOurSouls Nothing

-- "Objective - If each undefeated investigator has resigned: (-> R1)."
instance HasAbilities SaveOurSouls where
  getAbilities (SaveOurSouls a) =
    [ restricted a 1 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SaveOurSouls where
  runMessage msg a@(SaveOurSouls attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> SaveOurSouls <$> liftRunMessage msg attrs
