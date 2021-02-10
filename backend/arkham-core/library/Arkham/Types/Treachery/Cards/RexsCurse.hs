module Arkham.Types.Treachery.Cards.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RexsCurse = RexsCurse TreacheryAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryId -> Maybe InvestigatorId -> RexsCurse
rexsCurse uuid iid = RexsCurse $ weaknessAttrs uuid iid "02009"

instance HasModifiersFor env RexsCurse where
  getModifiersFor = noModifiersFor

instance HasActions env RexsCurse where
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env RexsCurse where
  runMessage msg t@(RexsCurse attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    Will (PassedSkillTest iid _ _ SkillTestInitiatorTarget{} _ _)
      | treacheryOnInvestigator iid attrs -> do
        let
          ability = (mkAbility (toSource attrs) 0 ForcedAbility)
            { abilityLimit = PlayerLimit PerTestOrAbility 1
            }
        usedAbilities <- map unUsedAbility <$> getList ()
        when ((iid, ability) `notElem` usedAbilities) $ do
          retainedMessages <- withQueue $ \queue ->
            let
              (remainingWillPass, queue') = flip span queue $ \case
                Will PassedSkillTest{} -> True
                _ -> False
              (before, after) = flip break queue' $ \case
                Ask iid' (ChooseOne [SkillTestApplyResults]) | iid == iid' ->
                  True
                _ -> False
              remaining = case after of
                [] -> []
                (_ : xs) -> xs
            in (before <> remaining, remainingWillPass)
          unshiftMessages
            $ retainedMessages
            <> [ ActivateCardAbilityAction iid ability
               , ReturnSkillTestRevealedTokens
               , AddSkillTestSubscriber (TreacheryTarget treacheryId)
               , DrawAnotherToken iid
               ]
        pure t
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ _ | tid == treacheryId ->
      t <$ unshiftMessage (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
