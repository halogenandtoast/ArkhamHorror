{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalker where

import Arkham.Import

import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype DaisyWalker = DaisyWalker Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalker :: DaisyWalker
daisyWalker =
  DaisyWalker $ (baseAttrs "01002" "Daisy Walker" Seeker stats [Miskatonic])
    { investigatorTomeActions = Just 1
    }
 where
  stats = Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }

becomesFailure :: Token -> Modifier -> Bool
becomesFailure token (ForcedTokenChange fromToken AutoFail) =
  token == fromToken
becomesFailure _ _ = False

instance ActionRunner env => HasActions env DaisyWalker where
  getActions i window (DaisyWalker attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env DaisyWalker where
  runMessage msg i@(DaisyWalker attrs@Attrs {..}) = case msg of
    ResetGame -> do
      attrs' <- runMessage msg attrs
      pure $ DaisyWalker $ attrs' { investigatorTomeActions = Just 1 }
    ActivateCardAbilityAction iid ability@Ability {..}
      | iid == investigatorId && sourceIsAsset abilitySource -> do
        let (AssetSource aid) = abilitySource
        traits <- asks (getSet aid)
        if Tome
          `elem` traits
          && fromJustNote "Must be set" investigatorTomeActions
          > 0
        then
          case abilityType of
            ForcedAbility -> DaisyWalker <$> runMessage msg attrs
            FastAbility _ -> DaisyWalker <$> runMessage msg attrs
            ReactionAbility _ -> DaisyWalker <$> runMessage msg attrs
            ActionAbility n actionType -> if n > 0
              then DaisyWalker <$> runMessage
                (ActivateCardAbilityAction
                  iid
                  (ability { abilityType = ActionAbility (n - 1) actionType })
                )
                (attrs
                  { investigatorTomeActions =
                    max 0 . subtract 1 <$> investigatorTomeActions
                  }
                )
              else DaisyWalker <$> runMessage msg attrs
        else
          DaisyWalker <$> runMessage msg attrs
    ResolveToken ElderSign iid | iid == investigatorId ->
      if any
          (becomesFailure ElderSign)
          (concat . toList $ investigatorModifiers)
        then i <$ unshiftMessage (ResolveToken AutoFail iid)
        else do
          i <$ runTest investigatorId (TokenValue ElderSign 0)
    PassedSkillTest iid _ _ (TokenTarget ElderSign) _ | iid == investigatorId ->
      do
        tomeCount <- asks $ unAssetCount . getCount (iid, [Tome])
        when (tomeCount > 0) $ unshiftMessage
          (Ask iid
          $ ChooseOne
              [ DrawCards iid tomeCount False
              , Continue "Do not use Daisy's ability"
              ]
          )
        pure i
    BeginRound -> DaisyWalker
      <$> runMessage msg (attrs { investigatorTomeActions = Just 1 })
    _ -> DaisyWalker <$> runMessage msg attrs
