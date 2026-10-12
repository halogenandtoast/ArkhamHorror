module Arkham.Homebrew.AgainstTheWendigo.Assets.NormanFalkner (normanFalkner) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (handOverGuideAbility)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (crossOut, record)

newtype NormanFalkner = NormanFalkner AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

normanFalkner :: AssetCard NormanFalkner
normanFalkner = allyWith NormanFalkner Cards.normanFalkner (2, 0) noSlots

instance HasAbilities NormanFalkner where
  getAbilities (NormanFalkner a) =
    [ mkAbility a 1 $ forced $ PhaseBegins #when #enemy
    , restricted a 2 OnSameLocation parleyAction_
    , handOverGuideAbility a 3
    ]
      -- Gone once the parley has sent him out of play: that removal raises the
      -- same leave-play window a discard does.
      <> [ onlyOnce
             $ mkAbility a 4
             $ forced
             $ oneOf [Matcher.AssetDefeated #when ByAny (be a), AssetDiscarded #when (be a)]
         | not (sentOutOfPlay a)
         ]

sentOutOfPlay :: AssetAttrs -> Bool
sentOutOfPlay a = toResultDefault False a.meta

instance RunMessage NormanFalkner where
  runMessage msg a@(NormanFalkner attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 4)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      removeFromGame attrs
      pure $ NormanFalkner $ setMeta True attrs
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      others <-
        select $ InvestigatorAt (locationWithInvestigator iid) <> NotInvestigator (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others \other -> takeControlOfAsset other attrs.id
      pure a
    -- He is out of play already: his other face is a story card, so the discard
    -- this window stands in front of is a removal from the game.
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      crossOut NormanIsAlive
      record YouLetNormanDie
      pure a
    _ -> NormanFalkner <$> liftRunMessage msg attrs
