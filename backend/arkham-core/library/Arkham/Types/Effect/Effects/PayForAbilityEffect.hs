module Arkham.Types.Effect.Effects.PayForAbilityEffect
  ( payForAbilityEffect
  , PayForAbilityEffect(..)
  )
where

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


import Arkham.Types.Action hiding (Ability, TakenAction)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Trait

newtype PayForAbilityEffect = PayForAbilityEffect (EffectAttrs `With` Payment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

payForAbilityEffect
  :: EffectId -> Maybe Ability -> Source -> Target -> PayForAbilityEffect
payForAbilityEffect eid mAbility source target =
  PayForAbilityEffect $ (`with` NoPayment) $ EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectCardCode = Nothing
    , effectMetadata = EffectAbility <$> mAbility
    , effectTraits = mempty
    , effectWindow = Nothing
    }

instance HasModifiersFor env PayForAbilityEffect where
  getModifiersFor _ target (PayForAbilityEffect (With EffectAttrs {..} _))
    | target == effectTarget = case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

matchTarget :: [Action] -> ActionTarget -> Action -> Bool
matchTarget takenActions (FirstOneOf as) action =
  action `elem` as && all (`notElem` takenActions) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCostModifier
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasList Action.TakenAction env InvestigatorId
     )
  => InvestigatorId
  -> Maybe Action
  -> m Int
getActionCostModifier _ Nothing = pure 0
getActionCostModifier iid (Just a) = do
  takenActions <- map unTakenAction <$> getList iid
  modifiers <-
    map modifierType
      <$> getModifiersFor (InvestigatorSource iid) (InvestigatorTarget iid) ()
  pure $ foldr (applyModifier takenActions) 0 modifiers
 where
  applyModifier takenActions (ActionCostOf match m) n =
    if matchTarget takenActions match a then n + m else n
  applyModifier _ _ n = n

instance
  ( HasQueue env
  , HasSet InScenarioInvestigatorId env ()
  , HasCostPayment env
  , HasSet Trait env Source
  , HasModifiersFor env ()
  , HasList Action.TakenAction env InvestigatorId
  )
  => RunMessage env PayForAbilityEffect where
  runMessage msg e@(PayForAbilityEffect (attrs `With` payments)) = case msg of
    CreatedEffect eid (Just (EffectAbility Ability {..})) source (InvestigatorTarget iid)
      | eid == toId attrs
      -> do
        unshiftMessage (PayAbilityCostFinished source iid)
        e <$ case abilityType of
          ForcedAbility -> pure ()
          FastAbility cost ->
            unshiftMessage (PayAbilityCost abilitySource iid Nothing cost)
          ReactionAbility cost ->
            unshiftMessage (PayAbilityCost abilitySource iid Nothing cost)
          ActionAbility mAction cost ->
            if mAction
                `notElem` [ Just Action.Fight
                          , Just Action.Evade
                          , Just Action.Resign
                          , Just Action.Parley
                          ]
              then unshiftMessages
                (PayAbilityCost abilitySource iid mAction cost
                : [ TakenAction iid action | action <- maybeToList mAction ]
                <> [CheckAttackOfOpportunity iid False]
                )
              else unshiftMessages
                (PayAbilityCost abilitySource iid mAction cost
                : [ TakenAction iid action | action <- maybeToList mAction ]
                )
    PayAbilityCost source iid mAction cost -> case cost of
      Costs xs ->
        e <$ unshiftMessages [ PayAbilityCost source iid mAction x | x <- xs ]
      UpTo 0 _ -> pure e
      UpTo n cost' -> do
        canAfford <- getCanAffordCost iid source mAction cost'
        e <$ when
          canAfford
          (unshiftMessage $ chooseOne
            iid
            [ Label
              "Pay dynamic cost"
              [ PayAbilityCost source iid mAction cost'
              , PayAbilityCost source iid mAction (UpTo (n - 1) cost')
              ]
            , Label "Done with dynamic cost" []
            ]
          )
      ExhaustCost target -> e <$ unshiftMessage (Exhaust target)
      DiscardCost target -> e <$ unshiftMessage (Discard target)
      DiscardCardCost cid -> e <$ unshiftMessage (DiscardCard iid cid)
      HorrorCost _ target x -> case target of
        InvestigatorTarget iid' | iid' == iid ->
          e <$ unshiftMessage
            (InvestigatorAssignDamage iid source DamageAny 0 x)
        AssetTarget aid -> e <$ unshiftMessage (AssetDamage aid source 0 x)
        _ -> error "can't target for horror cost"
      DamageCost _ target x -> case target of
        InvestigatorTarget iid' | iid' == iid ->
          e <$ unshiftMessage
            (InvestigatorAssignDamage iid source DamageAny x 0)
        AssetTarget aid -> e <$ unshiftMessage (AssetDamage aid source x 0)
        _ -> error "can't target for damage cost"
      ResourceCost x -> e <$ unshiftMessage (SpendResources iid x)
      ActionCost x -> do
        costModifier <- getActionCostModifier iid mAction
        let modifiedActionCost = max 0 (x + costModifier)
        e <$ unshiftMessage (SpendActions iid source modifiedActionCost)
      UseCost aid uType n ->
        e <$ unshiftMessage (SpendUses (AssetTarget aid) uType n)
      ClueCost x -> e <$ unshiftMessage (InvestigatorSpendClues iid x)
      GroupClueCost x Nothing -> do
        investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
        e <$ unshiftMessage (SpendClues x investigatorIds)
      GroupClueCost x (Just locationMatcher) -> do
        mLocationId <- getId @(Maybe LocationId) locationMatcher
        case mLocationId of
          Just lid -> do
            iids <- getSetList @InvestigatorId lid
            e <$ unshiftMessage (SpendClues x iids)
          Nothing -> error "could not pay cost"
      HandDiscardCost x mPlayerCardType traits skillTypes -> do
        handCards <- mapMaybe (preview _PlayerCard . unHandCard) <$> getList iid
        let
          cards = filter
            (and . sequence
              [ maybe (const True) (==) mPlayerCardType . pcCardType
              , (|| null traits) . not . null . intersection traits . pcTraits
              , (|| null skillTypes)
              . not
              . null
              . intersection (insertSet SkillWild skillTypes)
              . setFromList
              . pcSkills
              ]
            )
            handCards
        e <$ unshiftMessage
          (chooseN iid x [ DiscardCard iid (getCardId card) | card <- cards ])
      SkillIconCost x skillTypes -> do
        handCards <- mapMaybe (preview _PlayerCard . unHandCard) <$> getList iid
        let
          cards = filter ((> 0) . fst) $ map
            (toFst (count (`member` insertSet SkillWild skillTypes) . pcSkills))
            handCards
          cardMsgs = map
            (\(n, card) -> if n >= x
              then DiscardCard iid (getCardId card)
              else Run
                [ DiscardCard iid (getCardId card)
                , PayAbilityCost
                  source
                  iid
                  mAction
                  (SkillIconCost (x - n) skillTypes)
                ]
            )
            cards
        e <$ unshiftMessage (chooseOne iid cardMsgs)
      Free -> pure e
    PayAbilityCostFinished source iid -> case effectMetadata attrs of
      Just (EffectAbility Ability {..}) -> e <$ unshiftMessages
        [ DisableEffect $ toId attrs
        , UseCardAbility iid source abilityMetadata abilityIndex payments
        ]
      _ -> e <$ unshiftMessage (DisableEffect $ toId attrs)
    _ -> PayForAbilityEffect . (`with` payments) <$> runMessage msg attrs
