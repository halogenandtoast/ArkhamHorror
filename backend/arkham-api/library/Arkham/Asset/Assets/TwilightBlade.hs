module Arkham.Asset.Assets.TwilightBlade (twilightBlade) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Window (EffectWindow (..))
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  createWindowModifierEffect,
  modified_,
  modifyEach,
 )
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TwilightBlade = TwilightBlade AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightBlade :: AssetCard TwilightBlade
twilightBlade = asset TwilightBlade Cards.twilightBlade

instance HasModifiersFor TwilightBlade where
  getModifiersFor (TwilightBlade a) = for_ a.controller \iid -> do
    underDiana <- filterCards (CardWithOneOf [#event, #skill]) <$> field InvestigatorCardsUnderneath iid
    modifyEach a underDiana [AdditionalCost (exhaust a), AdditionalCostToCommit iid (exhaust a)]
    modified_ a iid
      $ concatMap (\c -> [AsIfInHandFor ForPlay c.id, CanCommitToSkillTestsAsIfInHand c]) underDiana

instance HasAbilities TwilightBlade where
  getAbilities (TwilightBlade a) = [controlled_ a 1 $ fightActionWithAlternate_ #willpower]

playedFromBeneath :: ModifierType
playedFromBeneath = InvestigatorModifier "playedFromBeneath"

drawIdOf :: Window -> Maybe CardDrawId
drawIdOf (windowType -> Window.WouldDrawCard _ drawId _) = Just drawId
drawIdOf (windowType -> Window.WouldDrawExactlyOneCard _ drawId _) = Just drawId
drawIdOf _ = Nothing

instance RunMessage TwilightBlade where
  runMessage msg a@(TwilightBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFightEnemyWithSkillChoice sid iid source [#combat, #willpower]
      pure a
    InitiatePlayCard iid card _ _ windows' _ | controlledBy attrs iid -> do
      -- the card may finish resolving long before the effect it set up cancels
      -- anything (Foresight), so tie the flag to the window it was played into
      markFromBeneath iid card case mapMaybe drawIdOf windows' of
        (drawId : _) -> EffectCardDrawWindow drawId
        [] -> EffectCardResolutionWindow card.id
      pure a
    SkillTestCommitCard iid card | controlledBy attrs iid -> do
      msid <- getSkillTestId
      markFromBeneath iid card
        $ maybe (EffectCardResolutionWindow card.id) EffectSkillTestWindow msid
      pure a
    _ -> TwilightBlade <$> liftRunMessage msg attrs
   where
    markFromBeneath iid card eWindow = do
      underDiana <- field InvestigatorCardsUnderneath iid
      when (card `elem` underDiana) do
        priority
          $ pushM
          $ createWindowModifierEffect eWindow attrs (CardIdTarget card.id) [playedFromBeneath]
