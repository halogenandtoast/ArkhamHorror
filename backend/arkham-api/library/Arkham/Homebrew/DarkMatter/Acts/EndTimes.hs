module Arkham.Homebrew.DarkMatter.Acts.EndTimes (endTimes) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scanAction_, scanAtYourLocation)
import Arkham.Matcher

newtype EndTimes = EndTimes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endTimes :: ActCard EndTimes
endTimes = act (1, A) EndTimes Cards.endTimes Nothing

instance HasAbilities EndTimes where
  getAbilities (EndTimes a) =
    [ restricted a 1 (exists $ You <> at_ Anywhere) scanAction_
    , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage EndTimes where
  runMessage msg a@(EndTimes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanAtYourLocation iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> EndTimes <$> liftRunMessage msg attrs
