module Arkham.Enemy.Cards.VoidChimeraTrueForm (voidChimeraTrueForm) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Modifiers (pattern CannotExpose)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype VoidChimeraTrueForm = VoidChimeraTrueForm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voidChimeraTrueForm :: EnemyCard VoidChimeraTrueForm
voidChimeraTrueForm = enemy VoidChimeraTrueForm Cards.voidChimeraTrueForm (4, PerPlayer 4, 4) (1, 1)

instance HasModifiersFor VoidChimeraTrueForm where
  getModifiersFor (VoidChimeraTrueForm a) = do
    anyOtherForm <-
      selectAny
        $ InPlayEnemy
        $ mapOneOf
          enemyIs
          [ Cards.voidChimeraFellbeak
          , Cards.voidChimeraEarsplitter
          , Cards.voidChimeraGorefeaster
          , Cards.voidChimeraFellhound
          ]
    when anyOtherForm $ modifySelect a Anyone [CannotExpose]

instance HasAbilities VoidChimeraTrueForm where
  getAbilities (VoidChimeraTrueForm a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ mapOneOf
        (CampaignEvent #after (Just You))
        [ "exposed[decoyVoidChimeraFellbeak]"
        , "exposed[decoyVoidChimeraEarsplitter]"
        , "exposed[decoyVoidChimeraGorefeaster]"
        , "exposed[decoyVoidChimeraFellhound]"
        ]

instance RunMessage VoidChimeraTrueForm where
  runMessage msg e@(VoidChimeraTrueForm attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let
        go = \case
          [] -> error "no form found"
          ((Window.windowType -> Window.CampaignEvent "exposed[decoyVoidChimeraFellbeak]" _ _) : _) -> do
            Cards.voidChimeraFellbeak
          ((Window.windowType -> Window.CampaignEvent "exposed[decoyVoidChimeraEarsplitter]" _ _) : _) -> do
            Cards.voidChimeraEarsplitter
          ((Window.windowType -> Window.CampaignEvent "exposed[decoyVoidChimeraGorefeaster]" _ _) : _) -> do
            Cards.voidChimeraGorefeaster
          ((Window.windowType -> Window.CampaignEvent "exposed[decoyVoidChimeraFellhound]" _ _) : _) -> do
            Cards.voidChimeraFellhound
          (_ : xs) -> go xs
        form = go ws

      withLocationOf iid $ createSetAsideEnemy_ form
      pure e
    _ -> VoidChimeraTrueForm <$> liftRunMessage msg attrs
