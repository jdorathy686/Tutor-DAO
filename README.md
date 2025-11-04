# 🎓 Tutor DAO

A decentralized autonomous organization for rewarding educators based on student reviews and community voting.

## 📋 Overview

Tutor DAO enables tutors to register their services, receive reviews from students, and earn rewards through community governance. The platform combines peer review systems with decentralized decision-making to fairly compensate quality educators.

## ✨ Features

- 👨‍🏫 **Tutor Registration**: Educators can register with their name and subject expertise
- ⭐ **Student Reviews**: Students can rate tutors (1-5 stars) and leave comments
- 🗳️ **DAO Governance**: Community members vote on reward proposals for tutors
- 💰 **Reward Distribution**: Successful proposals automatically distribute STX rewards
- 📊 **Rating System**: Automatic calculation of average tutor ratings
- 🏛️ **Treasury Management**: Community-funded reward pool

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run `clarinet check` to verify contract syntax
4. Run `clarinet test` to execute tests

## 📖 Usage Guide

### For Tutors 👨‍🏫

1. **Register as a tutor**:
   ```clarity
   (contract-call? .tutor-dao register-tutor "John Smith" "Mathematics")
   ```

2. **Check your profile**:
   ```clarity
   (contract-call? .tutor-dao get-tutor-by-address 'your-address)
   ```

### For Students 🎓

1. **Submit a review**:
   ```clarity
   (contract-call? .tutor-dao submit-review u1 u5 "Excellent teaching!")
   ```

2. **View tutor ratings**:
   ```clarity
   (contract-call? .tutor-dao get-tutor-average-rating u1)
   ```

### For DAO Members 🏛️

1. **Join the DAO**:
   ```clarity
   (contract-call? .tutor-dao join-dao)
   ```

2. **Fund the treasury**:
   ```clarity
   (contract-call? .tutor-dao fund-dao u1000000)
   ```

3. **Create reward proposal**:
   ```clarity
   (contract-call? .tutor-dao create-reward-proposal u1 u500000 "Reward for excellent reviews")
   ```

4. **Vote on proposals**:
   ```clarity
   (contract-call? .tutor-dao vote-on-proposal u1 true)
   ```

5. **Execute approved proposals**:
   ```clarity
   (contract-call? .tutor-dao execute-proposal u1)
   ```

## 🔧 Contract Functions

### Public Functions

- `register-tutor(name, subject)` - Register as a tutor
- `join-dao()` - Become a DAO member
- `submit-review(tutor-id, rating, comment)` - Review a tutor
- `create-reward-proposal(tutor-id, amount, description)` - Propose tutor reward
- `vote-on-proposal(proposal-id, vote-for)` - Vote on proposals
- `execute-proposal(proposal-id)` -
