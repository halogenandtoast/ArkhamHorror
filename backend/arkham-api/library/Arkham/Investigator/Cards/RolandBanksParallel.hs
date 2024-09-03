module Arkham.Investigator.Cards.RolandBanksParallel (
  rolandBanksParallel,
  RolandBanksParallel (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Matcher hiding (PlayCard)
import Arkham.Movement
import Arkham.Name
import Arkham.Projection
import Arkham.Slot
import Control.Lens (each)

newtype RolandBanksParallel = RolandBanksParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rolandBanksParallel :: InvestigatorCard RolandBanksParallel
rolandBanksParallel =
  investigator RolandBanksParallel Cards.rolandBanksParallel
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities RolandBanksParallel where
  getAbilities (RolandBanksParallel a) = [playerLimit PerGame $ restrictedAbility a 1 Self $ FastAbility Free]

instance HasChaosTokenValue RolandBanksParallel where
  getChaosTokenValue iid ElderSign (RolandBanksParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RolandBanksParallel where
  runMessage msg i@(RolandBanksParallel attrs) = runQueueT $ case msg of
    Do BeginRound -> do
      attrs' <- liftRunMessage msg attrs
      pure . RolandBanksParallel $ attrs' & setMeta defaultMeta
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1) $ assetControlledBy iid <> AssetWithTitle "Directive"
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      flipOverBy iid (attrs.ability 1) aid
      pure i
    ElderSignEffect (is attrs -> True) -> do
      directives <- selectWithField AssetName $ assetControlledBy attrs.id <> AssetWithTitle "Directive"
      let internalMeta = toResultDefault defaultMeta attrs.meta
      let availableDirectives = filter (\(_, b) -> directiveNameToLabel b `notElem` ignoredDirectives internalMeta) directives

      when (notNull availableDirectives) $ do
        chooseOne
          attrs.id
          [ Label
            (fromMaybe "Unknown" $ nameSubtitle name)
            [HandleTargetChoice attrs.id (ChaosTokenEffectSource ElderSign) (AssetTarget directive)]
          | (directive, name) <- availableDirectives
          ]

      pure i
    HandleTargetChoice (is attrs -> True) (ChaosTokenEffectSource ElderSign) (AssetTarget aid) -> do
      label <- fieldMap AssetName directiveNameToLabel aid
      let internalMeta = toResultDefault defaultMeta attrs.meta
      pure
        . RolandBanksParallel
        $ attrs
        & setMeta
          ( internalMeta {ignoredDirectives = label : ignoredDirectives internalMeta}
          )
    FightEnemy _ (is attrs -> True) _ _ _ _ False -> do
      let internalMeta = toResultDefault defaultMeta attrs.meta
      attrs' <- liftRunMessage msg attrs
      pure
        . RolandBanksParallel
        $ attrs'
        & setMeta
          ( internalMeta {dueDiligence = dueDiligence internalMeta + 1}
          )
    MoveTo movement | isTarget attrs (moveTarget movement) -> do
      let internalMeta = toResultDefault defaultMeta attrs.meta
      attrs' <- liftRunMessage msg attrs
      case moveDestination movement of
        ToLocation _ ->
          pure
            . RolandBanksParallel
            $ attrs'
            & setMeta
              ( internalMeta {leaveNoDoubt = leaveNoDoubt internalMeta + 1}
              )
        _ -> pure . RolandBanksParallel $ attrs'
    PlayCard (is attrs -> True) _ _ _ _ False -> do
      let internalMeta = toResultDefault defaultMeta attrs.meta
      attrs' <- liftRunMessage msg attrs
      pure
        . RolandBanksParallel
        $ attrs'
        & setMeta
          ( internalMeta {redTape = redTape internalMeta + 1}
          )
    Flip (is attrs -> True) _ (AssetTarget aid) -> do
      attrs' <- liftRunMessage msg attrs
      isConsultExperts <- aid <=~> assetIs Assets.directiveConsultExperts
      if isConsultExperts
        then do
          push $ RefillSlots attrs.id
          pure
            . RolandBanksParallel
            $ attrs'
            & (slotsL . each %~ filter ((not . isSlotSource (toSource aid))))
        else pure $ RolandBanksParallel attrs'
    _ -> do
      discoveredClue <- notNull <$> getHistoryField RoundHistory attrs.id HistoryCluesDiscovered
      let internalMeta = toResultDefault defaultMeta attrs.meta
      let internalMeta' = internalMeta {seekTheTruth = not discoveredClue}
      RolandBanksParallel . (setMeta internalMeta') <$> liftRunMessage msg attrs
