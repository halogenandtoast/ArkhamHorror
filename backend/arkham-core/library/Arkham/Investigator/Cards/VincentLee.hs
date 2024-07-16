module Arkham.Investigator.Cards.VincentLee (vincentLee, VincentLee (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Helpers
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCard)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills
import Arkham.Window (Window, WindowType (Healed), windowType)

newtype VincentLee = VincentLee InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

vincentLee :: InvestigatorCard VincentLee
vincentLee =
  investigator VincentLee Cards.vincentLee
    $ Stats {health = 9, sanity = 6, willpower = 3, intellect = 4, combat = 3, agility = 1}

instance HasAbilities VincentLee where
  getAbilities (VincentLee a) =
    [ restrictedAbility a 1 (Self <> exists (SetAsideCardMatch $ cardIs Skills.onTheMend))
        $ freeReaction
        $ oneOf
          [ AssetHealed
              #after
              #damage
              (AssetControlledBy (affectsOthers $ not_ $ HandWith $ HasCard $ cardIs Skills.onTheMend))
              (SourceIsCardEffect <> SourceOwnedBy You)
          , InvestigatorHealed
              #after
              #damage
              (not_ $ HandWith $ HasCard $ cardIs Skills.onTheMend)
              (SourceIsCardEffect <> SourceOwnedBy You)
          ]
    ]

instance HasChaosTokenValue VincentLee where
  getChaosTokenValue iid ElderSign (VincentLee attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getOnTheMend :: [Window] -> Either AssetId InvestigatorId
getOnTheMend [] = error "no one mending"
getOnTheMend ((windowType -> Healed _ (InvestigatorTarget iid) _ _) : _) = Right iid
getOnTheMend ((windowType -> Healed _ (AssetTarget aid) _ _) : _) = Left aid
getOnTheMend (_ : rest) = getOnTheMend rest

instance RunMessage VincentLee where
  runMessage msg i@(VincentLee attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      n <- getPlayerCount
      c <- genCards $ replicate n Skills.onTheMend
      push $ SetAsideCards c
      pure
        $ VincentLee
        $ attrs'
        & deckL
        %~ Deck
        . filter (not . (`cardMatch` cardIs Skills.onTheMend))
        . unDeck
    UseCardAbility _iid (isSource attrs -> True) 1 (getOnTheMend -> mending) _ -> do
      s <- getSetAsideCard Skills.onTheMend
      iid' <- either (\aid -> fieldJust AssetController aid) pure mending
      push $ AddToHand iid' [s]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      targets <-
        (<>)
          <$> selectTargets (HealableInvestigator (toSource attrs) #damage $ colocatedWith iid)
          <*> selectTargets
            (HealableAsset (toSource attrs) #damage $ (AssetControlledBy $ affectsOthers $ colocatedWith iid))
      when (notNull targets) do
        chooseOne iid $ Label "Do not heal" []
          : [targetLabel target [HealDamage target (toSource attrs) 1] | target <- targets]

      pure i
    _ -> VincentLee <$> liftRunMessage msg attrs
